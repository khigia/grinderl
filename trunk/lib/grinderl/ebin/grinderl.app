{application, grinderl, [
    {description, "Grinderl is a testing tool."},
    {vsn, "0.1.0"},
    {modules, [
        grinderl,
        grd_grinderl_sup,
        grd_random_srv,
        grd_stress_srv,
        grd_worker_srv,
        grd_util,
        grd_extcmd
    ]},
    {registered, [
        grd_grinderl_sup,
        grd_random_srv,
        grd_stress_srv
    ]},
    {applications, [
        kernel,
        stdlib,
        sasl
    ]},
    {mod, {grinderl, []}},
    {env, [
    ]}
]}.
