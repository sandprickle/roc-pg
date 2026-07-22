app [main!] {
	pf: platform "../../basic-cli/platform/main.roc",
	pg: "../src/main.roc",
}

import pf.Stdout
import pf.Tcp
import pg.Pg exposing [Cmd, Client]
import pg.Result

Model : {
	client : Client,
}

main! = |_args| {
	Stdout.line!("Start!")?

	client = Client.connect!({
		host: "localhost",
		port: 5432,
		user: "postgres",
		auth: None,
		database: "postgres",
		tcp_connect!: |host, port| Tcp.connect!(host, port),
	})?

	Stdout.line!("Connected!")?

	rows = Cmd.new(
		\\ select $1 as name, $2 as age
		\\ union all
		\\ select 'Julio' as name, 23 as age
		,
	)
		.bind([Cmd.str("John"), Cmd.u8(32)])
		.expect_n(
			{
				name: Result.str("name"),
				age: Result.u8("age"),
			}.Result,
		)
		->(|c| client.command!(c))?

	Stdout.line!("Received Rows:")?
	for row in rows {
		Stdout.line!(Str.inspect(row))?
	}

	Ok({})
}
