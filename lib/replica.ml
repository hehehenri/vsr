open Types

type state =
  | Normal
  | ViewChange 
  | Recovering

module type OperationExecutor = sig
  type operation
  type result

  val execute : operation -> result
end

module Make (N : Network.S) (Op : OperationExecutor) = struct
  type replica = {
    id : ReplicaId.t;
    config : ReplicaId.t list;
    mutable state : state;
    mutable view_number : ViewNumber.t;
    mutable op_number : OpNumber.t;
    mutable commit_number : OpNumber.t;
    mutable request_log : Op.operation Message.request OpMap.t;
    mutable client_map : (Op.operation, Op.result) Types.client_map;
    mutable prepare_ok_acks : ReplicaSet.t OpMap.t;
    mutable start_view_change_acks : ReplicaSet.t ViewMap.t;
    mutable do_view_change_acks : ReplicaSet.t ViewMap.t;
    mutable recovery_nonce : int option;
    mutable recovery_responses : ReplicaSet.t ViewMap.t;
    network : (Op.operation, Op.result) N.t;
  }

  let init_replica id config =
    let config = List.map (fun id -> ReplicaId.of_int id) config in
    let request_log = OpMap.empty in
    let client_map = ClientMap.empty in
    {
      id = ReplicaId.of_int id;
      state = Normal;
      view_number = ViewNumber.init ();
      config;
      op_number = OpNumber.init ();
      commit_number = OpNumber.init ();
      request_log;
      client_map;
      network = N.create config;
      prepare_ok_acks = OpMap.empty;
      start_view_change_acks = ViewMap.empty;
      do_view_change_acks = ViewMap.empty;
      recovery_nonce = None;
      recovery_responses = ViewMap.empty;
    }

  let get_primary_id replica =
    let view_number = ViewNumber.to_int replica.view_number in
    let length = List.length replica.config in
    assert (length <> 0);
    let primary_index = view_number mod length in
    List.nth replica.config primary_index

  let is_primary replica =
    let primary_id = get_primary_id replica in
    primary_id = replica.id

  let next_primary replica =
    let current_primary = get_primary_id replica in
    ReplicaId.succ current_primary

  let is_backup replica = not @@ is_primary replica
  let is_normal replica = replica.state = Normal
  let is_recovering replica = replica.state = Recovering

  let quorum replica =
    let n = List.length replica.config in
    (n / 2) + 1

  let commit replica (op : OpNumber.t) =
    match OpMap.find_opt op replica.request_log with
    | Some req -> Op.execute req.op
    | None -> failwith "op not found in log"

  let handle_request replica
      (request : Op.operation Message.request) =
    assert (is_primary replica);

    let continue () =
      replica.op_number <- OpNumber.succ replica.op_number;
      replica.request_log <-
        OpMap.add replica.op_number request replica.request_log;
      replica.client_map <-
        ClientMap.update request.client_id
          (fun _ ->
            Some
              {
                client_id = request.client_id;
                request_number = request.request_number;
                response = None;
              })
          replica.client_map;

      let prepare =
        Message.ReplicaMessage (Message.Prepare
          {
            view_number = replica.view_number;
            op_number = replica.op_number;
            commit_number = replica.commit_number;
            request;
          })
      in
      N.broadcast replica.network prepare
    in

    let latest_request =
      ClientMap.find_opt request.client_id replica.client_map
    in
    match latest_request with
    | Some latest_request ->
        if request.request_number < latest_request.request_number then
          ()
        else if request.request_number = latest_request.request_number then
          (match latest_request.response with
          | Some result ->
              let response = 
                Message.ClientMessage (Message.Response {
                  client_id = request.client_id;
                  request_number = request.request_number;
                  result;
                }) in
              N.send replica.network (Client request.client_id) response
          | None -> continue ())
        else continue ()
    | None -> continue ()

  let request_state_transfer replica =
    assert (is_normal replica);
    let get_state = Message.ReplicaMessage (Message.GetState {
      view_number = replica.view_number;
      op_number = replica.op_number;
      replica_id = replica.id;
    }) in
    N.broadcast replica.network get_state

  let handle_get_state replica (msg : Message.get_state) =
    if is_normal replica && msg.view_number = replica.view_number then
      let new_state = Message.ReplicaMessage (Message.NewState {
        view_number = replica.view_number;
        log = OpMap.filter 
          (fun op_num _ -> OpNumber.compare op_num msg.op_number > 0) 
          replica.request_log;
        op_number = replica.op_number;
        commit_number = replica.commit_number;
      }) in
      N.send replica.network (Single msg.replica_id) new_state

  let handle_new_state replica (msg : Op.operation Message.new_state) =
    if msg.view_number = replica.view_number then begin
      OpMap.iter (fun op_num op ->
        if not (OpMap.mem op_num replica.request_log) then
          replica.request_log <- OpMap.add op_num op replica.request_log
      ) msg.log;
      
      if OpNumber.compare msg.op_number replica.op_number > 0 then
        replica.op_number <- msg.op_number;
      
      if OpNumber.compare msg.commit_number replica.commit_number > 0 then begin
        let rec execute_ops op_n =
          if OpNumber.compare op_n msg.commit_number <= 0 then begin
            match OpMap.find_opt op_n replica.request_log with
            | Some _ -> 
                ignore @@ commit replica op_n;
                execute_ops (OpNumber.succ op_n)
            | None -> ()
          end
        in
        execute_ops (OpNumber.succ replica.commit_number);
        replica.commit_number <- msg.commit_number
      end
    end

  let handle_prepare replica (prep : Op.operation Message.prepare) =
    assert (is_backup replica);
    assert (is_normal replica);
    let is_up_to_date = OpNumber.compare prep.op_number (OpNumber.succ replica.op_number) <= 0 in

    if not is_up_to_date then begin
      request_state_transfer replica;
      (* this will be processed whenever we caught up with state transfer *)
      ()
    end else begin
      assert (OpNumber.compare (OpNumber.succ replica.op_number) prep.op_number = 0);
      replica.op_number <- prep.op_number;
      replica.request_log <-
        OpMap.update prep.op_number
          (fun _ -> Some prep.request)
          replica.request_log;
      replica.client_map <-
        ClientMap.add prep.request.client_id
          {
            client_id = prep.request.client_id;
            request_number = prep.request.request_number;
            response = None;
          }
          replica.client_map;

      let prepare_ok =
        Message.ReplicaMessage (Message.PrepareOk
          {
            view_number = replica.view_number;
            op_number = replica.op_number;
            replica_id = replica.id;
          })
      in
      let primary_id = get_primary_id replica in
      N.send replica.network (Single primary_id) prepare_ok
    end

  let handle_prepare_ok replica
      (prep_ok : Message.prepare_ok) =
    assert (is_primary replica);
    
    let acks =
      match OpMap.find_opt prep_ok.op_number replica.prepare_ok_acks with
      | Some x -> x
      | None -> ReplicaSet.empty
    in

    let acks = ReplicaSet.add prep_ok.replica_id acks in
    replica.prepare_ok_acks <-
      OpMap.add replica.op_number acks replica.prepare_ok_acks;

    let total_acks = ReplicaSet.cardinal acks in
    if total_acks = quorum replica then begin
      replica.commit_number <- prep_ok.op_number;
      let result = commit replica prep_ok.op_number in
      
      match OpMap.find_opt prep_ok.op_number replica.request_log with
      | Some request ->
          let client_entry = ClientMap.find_opt request.client_id replica.client_map in
          (match client_entry with
          | Some entry when entry.request_number = request.request_number ->
              replica.client_map <- ClientMap.add request.client_id 
                { entry with response = Some result } 
                replica.client_map;
              
              let response = 
                Message.ClientMessage (Message.Response {
                  client_id = request.client_id;
                  request_number = request.request_number;
                  result;
                }) in
              N.send replica.network (Client request.client_id) response
          | _ -> ());

          let commit =
            Message.ReplicaMessage (Message.Commit
              { view_number = replica.view_number; commit_number = prep_ok.op_number })
          in
          N.broadcast replica.network commit
      | None -> ()
    end

  let handle_commit replica (message : Message.commit) =
    assert (is_backup replica);
    assert (is_normal replica);
    
    let open Message in
    OpMap.find_opt message.commit_number replica.request_log
    |> Option.iter (fun req -> ignore @@ Op.execute req.op)

  let handle_start_view_change replica
      (msg : Message.start_view_change) =
    if msg.view_number > replica.view_number then (
      let acks =
        match
          ViewMap.find_opt msg.view_number replica.start_view_change_acks
        with
        | Some acks -> acks
        | None -> ReplicaSet.empty
      in

      let acks = ReplicaSet.add msg.replica_id acks in
      replica.start_view_change_acks <-
        ViewMap.add msg.view_number acks replica.start_view_change_acks;

      let total_acks = ReplicaSet.cardinal acks in
      if total_acks = quorum replica then
        let next_primary = next_primary replica in
        let do_view_change =
          Message.ReplicaMessage (Message.DoViewChange
            {
              view_number = ViewNumber.succ replica.view_number;
              log = replica.request_log;
              last_view_number = replica.view_number;
              op_number = replica.op_number;
              commit_number = replica.commit_number;
            })
        in
        N.send replica.network (Single next_primary) do_view_change)

  let handle_do_view_change (replica : replica)
      (message : Op.operation Message.do_view_change) =
    assert (is_normal replica);
    assert (is_primary replica);
    
    let acks = match ViewMap.find_opt message.view_number replica.do_view_change_acks with
      | Some x -> x
      | None -> ReplicaSet.empty
    in
    let acks = ReplicaSet.add replica.id acks in
    replica.do_view_change_acks <- ViewMap.add message.view_number acks replica.do_view_change_acks;

    let total_acks = ReplicaSet.cardinal acks in
    if total_acks = quorum replica then begin
      replica.view_number <- message.view_number;
      replica.op_number <- message.op_number;
      replica.commit_number <- message.commit_number;
      replica.request_log <- message.log;

      let new_state = Message.ReplicaMessage (Message.NewState {
        view_number = message.view_number;
        log = message.log;
        op_number = message.op_number;
        commit_number = message.commit_number;
      }) in
      N.broadcast replica.network new_state;

      let rec execute_ops op_n =
        if OpNumber.compare op_n message.commit_number <= 0 then begin
          match OpMap.find_opt op_n replica.request_log with
          | Some req -> 
              let result = commit replica op_n in
              replica.client_map <- ClientMap.update req.client_id
                (fun _ -> Some {
                  client_id = req.client_id;
                  request_number = req.request_number;
                  response = Some result;
                })
                replica.client_map;
              let response = Message.ClientMessage (Message.Response {
                client_id = req.client_id;
                request_number = req.request_number;
                result;
              }) in
              N.send replica.network (Client req.client_id) response;
              execute_ops (OpNumber.succ op_n)
          | None -> ()
        end
      in
      execute_ops (OpNumber.succ replica.commit_number)
    end

  let handle_recovery (replica : replica) (msg : Message.recovery) =
    assert (is_normal replica);
    let response = 
      Message.ReplicaMessage (Message.RecoveryResponse {
        replica_id = replica.id;
        view_number = replica.view_number;
        nonce = msg.nonce;
        response_type = if is_primary replica then
          `Primary {
            log = replica.request_log;
            op_number = replica.op_number;
            commit_number = replica.commit_number;
          }
        else `NonPrimary
      })
    in
    N.send replica.network (Single msg.replica_id) response

  let handle_recovery_response (replica : replica) (msg : Op.operation Message.recovery_response) =
    assert (is_recovering replica);
    match replica.recovery_nonce with
    | None -> ()
    | Some nonce when nonce <> msg.nonce -> ()
    | Some _ ->
        let responses = match ViewMap.find_opt msg.view_number replica.recovery_responses with
          | None -> ReplicaSet.empty
          | Some s -> s
        in
        let responses = ReplicaSet.add msg.replica_id responses in
        replica.recovery_responses <- ViewMap.add msg.view_number responses replica.recovery_responses;
        
        let latest_view = ViewMap.max_binding_opt replica.recovery_responses
          |> Option.map fst
          |> Option.value ~default:replica.view_number
        in
        let responses_for_view = ViewMap.find_opt latest_view replica.recovery_responses
          |> Option.value ~default:ReplicaSet.empty
        in
        if ReplicaSet.cardinal responses_for_view >= quorum replica then begin
          let primary_id = get_primary_id { replica with view_number = latest_view } in
          match msg.response_type with
          | `Primary data ->
            if msg.replica_id = primary_id then
            replica.view_number <- msg.view_number;
            replica.op_number <- data.op_number;
            replica.commit_number <- data.commit_number;
            replica.request_log <- data.log;
            replica.state <- Normal;
            replica.recovery_nonce <- None;
            replica.recovery_responses <- ViewMap.empty;
            ()
          | `NonPrimary -> ()
        end

  let handle_message replica (message : (Op.operation, Op.result) Message.message) =
    match message with
    | Message.ClientMessage client_msg ->
        (match replica.state, client_msg with
        | Normal, Message.Request req -> handle_request replica req
        | Normal, Message.Response _ -> ()
        | ViewChange, _ -> ()
        | Recovering, _ -> ())
    | Message.ReplicaMessage replica_msg ->
        (match replica.state, replica_msg with
        | Normal, Message.Prepare prep -> handle_prepare replica prep
        | Normal, Message.PrepareOk prep_ok -> handle_prepare_ok replica prep_ok
        | Normal, Message.Commit commit -> handle_commit replica commit
        | Normal, Message.StartViewChange svc -> handle_start_view_change replica svc
        | Normal, Message.DoViewChange dvc -> handle_do_view_change replica dvc
        | Normal, Message.Recovery recovery -> handle_recovery replica recovery
        | Normal, Message.RecoveryResponse _ -> () (* ignore recovery responses in normal state *)
        | Normal, Message.GetState get_state -> handle_get_state replica get_state
        | Normal, Message.NewState new_state -> handle_new_state replica new_state
        
        | ViewChange, Message.StartViewChange svc -> handle_start_view_change replica svc
        | ViewChange, Message.DoViewChange dvc -> handle_do_view_change replica dvc
        | ViewChange, Message.Recovery recovery -> handle_recovery replica recovery
        | ViewChange, Message.RecoveryResponse _ -> () (* ignore recovery responses in view change *)
        | ViewChange, Message.Prepare _ -> () (* ignore normal operation messages during view change *)
        | ViewChange, Message.PrepareOk _ -> ()
        | ViewChange, Message.Commit _ -> ()
        | ViewChange, Message.GetState _ -> ()
        | ViewChange, Message.NewState _ -> ()
        
        | Recovering, Message.Recovery recovery -> handle_recovery replica recovery
        | Recovering, Message.RecoveryResponse resp -> handle_recovery_response replica resp
        | Recovering, Message.Prepare _ -> () (* ignore normal operation messages during recovery *)
        | Recovering, Message.PrepareOk _ -> ()
        | Recovering, Message.Commit _ -> ()
        | Recovering, Message.StartViewChange _ -> ()
        | Recovering, Message.DoViewChange _ -> ()
        | Recovering, Message.GetState _ -> ()
        | Recovering, Message.NewState _ -> ())
end
