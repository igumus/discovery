{application,
 discovery,
 [
  {vsn, "1.0.0"},
  {modules, [discovery_app, discovery_sup, discovery_server, discovery_log]},
  {applications, [crypto]},
  {registered, []},
  {mod, {discovery_app, []}},
  {env, 
   [    
    {inbound, 5000},
    {outbound, 6000},
    {interface, "eth0"},
    {interval, 30000}
   ]}   
 ]
}.
