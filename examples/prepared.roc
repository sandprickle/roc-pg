app [main!] {
	pf: platform "https://github.com/niclas-ahden/basic-cli/releases/download/0.22.1/DobkAk7zNyqAgqh2Riaj5c5DtWtKhd5iVYE5RFa6izcd.tar.zst",
	pg: "../src/main.roc",
}

import pf.Stdout
import pf.Tcp
import pg.Pg
import pg.Result

main! = |_| {
	client = Pg.Client.connect!({
		stream: Tcp.connect!("localhost", 5432)?,
		user: "postgres",
		auth: None,
		database: "postgres",
	})?
	Stdout.line!("Connected!")?

	add_cmd = client.prepare!({
		name: "add",
		sql: "select $1::int + $2::int as result",
	})?

	add_and_print! = |a, b| {
		result = add_cmd
			.bind([Pg.Cmd.u8(a), Pg.Cmd.u8(b)])
			.expect_1(Result.u8("result"))
			.send!(client)?

		a_str = (a).to_str()
		b_str = (b).to_str()
		result_str = (result).to_str()

		Stdout.line!("${a_str} + ${b_str} = ${result_str}")
	}

	add_and_print!(1, 2)?
	add_and_print!(11, 31)?

	Ok({})
}
