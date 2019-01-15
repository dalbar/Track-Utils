let group_by to_key list =
  let tbl = Hashtbl.create 10000 in
  List.iter
    (fun entry ->
      let key = to_key entry in
      let tbl_value = CCHashtbl.get tbl key in
      if CCOpt.is_none tbl_value then Hashtbl.add tbl key [entry]
      else Hashtbl.replace tbl key (entry :: CCOpt.get_exn tbl_value) )
    list ;
  CCHashtbl.values_list tbl
