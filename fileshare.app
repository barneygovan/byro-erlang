{application, fileshare,
  [{description, "Simple File Sharing"},
   {vsn, "1.0"},
   {modules, [fileshare_app, fileshare_supervisor, fileshare_server,
              files, lib_md5, lib_sha1]},
   {registered,[fileshare_server, fileshare_supervisor]},
   {applications, [kernel,stdlib]},
   {mod, {fileshare_app, []}},
   {start_phases, []}
  ]}.
