app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
	pg: "../src/main.roc",
}

import pf.Stdout
import pf.Tcp
import pg.Pg
import pg.Result

main! = |_| {
	client = Pg.Client.connect!({
		stream: Tcp.connect!("localhost", 5432, 10_000)?,
		user: "postgres",
		auth: None,
		database: "postgres",
	})?
	Stdout.line!("Connected!")?

	result = Pg.Batch.succeed(
		|hi| |eleven| |thirty_one| { hi, forty_two: eleven + thirty_one },
	)
		.with_cmd(
			Pg.Cmd.new("select 'hi' as value")
				.expect_1(Result.str("value")),
		)
		.with_cmd(
			Pg.Cmd.new("select $1::int as value")
				.bind([Pg.Cmd.u8(11)])
				.expect_1(Result.u8("value")),
		)
		.with_cmd(
			Pg.Cmd.new("select $1::int as value")
				.bind([Pg.Cmd.u8(31)])
				.expect_1(Result.u8("value")),
		)
		.send!(client)?

	str42 = result.forty_two.to_str()
	Stdout.line!("${result.hi} ${str42}")?

	result_seq = Pg.Batch.sequence(
		List.from_iter(0..=20)
			.map(
				|num|
					Pg.Cmd.new("select $1::int as value")
						.bind([Pg.Cmd.u8(num)])
						.expect_1(Result.u8("value")),
			),
	).send!(client)?

	result_seq_str = result_seq
		.map(|num| num.to_str())
		|> Str.join_with(", ")

	Stdout.line!(result_seq_str)?

	Ok({})
}
