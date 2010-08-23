{application, erlcraft,
 [{description, "erlcraft"},
  {vsn, "0.01"},
  {modules, [
    erlcraft,
    erlcraft_app,
    erlcraft_sup,
    erlcraft_server,
    erlcraft_deps
  ]},
  {registered, []},
  {mod, {erlcraft_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
