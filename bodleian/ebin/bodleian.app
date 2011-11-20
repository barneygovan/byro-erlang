{application, bodleian,
 [{description, "A simple document store"},
  {vsn, "0.1.0"},
  {modules, [
              bds_app,
              bds_sup,
              bds_event,
              bds_event_logger,
              bodleian
            ]},
  {registered, [bds_sup]},
  {applications, [kernel, stdlib]},
  {mod, {bds_app, []}}
 ]}.