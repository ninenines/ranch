{application, 'ranch', [
	{description, "debug gen_tcp_socket."},
	{vsn, "0.0.1"},
	{modules, ['gen_tcp_socket']},
	{registered, []},
	{applications, [kernel,stdlib]},
	{env, []}
]}.