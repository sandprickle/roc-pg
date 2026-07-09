import Cmd
import Result
import Util exposing [map_try]

Batch(a, err) :: Params(
	{
		decode : List(Result) -> Try(
			{
				value : a,
				rest : List(Result),
			},
			[
				MissingCmdResult(U64),
				ExpectErr(err),
			],
		),
	},
).{
	BatchedCmd : Cmd.Params({}, [ReuseSql(U64)])

	reuse_name : U64 -> Str
	reuse_name = |index| "b[${index.to_str()}]"

	succeed : _ -> Batch(_, _)
	succeed = |value| Batch.(
		{
			commands: List.with_capacity(5),
			seen_sql: Dict.with_capacity(5),
			decode: |rest| Ok({ value, rest }),
		},
	)

	with_cmd : Batch((a -> b), err), Cmd(a, err) -> Batch(b, err)
	with_cmd = |Batch.(batch), cmd| {
		{ seen_sql, new_cmd, new_index } = add_cmd(batch, cmd)
		commands = batch.commands.append(new_cmd)

		decode = |results| {
			{ value: fn, rest } = batch.decode(results)?

			match rest {
				[next, ..] => {
					a = Cmd.decode(next, cmd).map_err(|e| ExpectErr(e))?
					Ok({ value: fn(a), rest: rest.drop_first(1) })
				}
				_ => Err(MissingCmdResult(new_index))
			}
		}

		Batch.({ commands, seen_sql, decode })
	}

	sequence : List(Cmd(a, err)) -> Batch(List(a), err)
	sequence = |cmds| {
		count = cmds.len()

		init = {
			commands: List.with_capacity(count),
			seen_sql: Dict.with_capacity(smallest(10, count)),
		}

		batch = cmds.fold(
			init,
			|b, cmd| {
				{ seen_sql, new_cmd, new_index: _ } = add_cmd(b, cmd)
				{ seen_sql, commands: b.commands.append(new_cmd) }
			},
		)

		decode = |results| List.map2(
			results,
			cmds,
			Cmd.decode,
		)->map_try(|r| r)
			.map_ok(|value| { value, rest: [] })
			.map_err(|e| ExpectErr(e))

		Batch.(
			{
				commands: batch.commands,
				seen_sql: batch.seen_sql,
				decode,
			},
		)
	}

	params : Batch(a, err) -> Params(_)
	params = |Batch.(batch)| batch
}

Params(a) : {
	commands : List(BatchedCmd),
	seen_sql : SeenSql,
	..a,
}

SeenSql : Dict(Str, { index : U64, reused : Bool })

smallest : num, num -> num
	where [
		num.is_lt : num, num -> Bool,
		num.from_numeral : Numeral -> Try(num, [InvalidNumeral(Str)]),
	]
smallest = |a, b| if a < b a else b

add_cmd : Params(p),
Cmd(a, err) -> {
	seen_sql : SeenSql,
	new_cmd : BatchedCmd,
	new_index : U64,
}
add_cmd = |batch, cmd| {
	cmd_params = cmd.params()
	new_index = batch.commands.len()

	match cmd_params.kind {
		SqlCmd(sql) => match batch.seen_sql.get(sql) {
			Err(KeyNotFound) => {
				entry = { index: new_index, reused: Bool.False }
				seen_sql = batch.seen_sql.insert(sql, entry)
				new_cmd = SqlCmd(sql)->batched_cmd(cmd_params)

				{ seen_sql, new_cmd, new_index }
			}

			Ok({ index, reused }) => {
				seen_sql = if reused {
					batch.seen_sql
				} else {
					entry = { index, reused: Bool.True }
					batch.seen_sql.insert(sql, entry)
				}

				new_cmd = ReuseSql(index)->batched_cmd(cmd_params)

				{ seen_sql, new_cmd, new_index }
			}
		}

		PreparedCmd(prep) => {
			new_cmd = PreparedCmd(prep)->batched_cmd(cmd_params)
			{ seen_sql: batch.seen_sql, new_cmd, new_index }
		}
	}
}

batched_cmd : Cmd.Kind([ReuseSql(U64)]), Cmd.Params({}, []) -> BatchedCmd
batched_cmd = |kind, cmd_params| {
	kind,
	bindings: cmd_params.bindings,
	limit: cmd_params.limit,
}
