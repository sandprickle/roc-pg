import Result as InternalResult
import Cmd as InternalCmd
import Batch as InternalBatch

Pg :: [].{

	Cmd(a, err) :: InternalCmd(a, err).{

		new : Str -> Cmd(Result, [])
		new = |sql| Cmd.(InternalCmd.from_sql(sql))

		expect_n : Cmd(Result, []),
		Result.Decode(a, err) -> Cmd(
			List(a),
			[FieldNotFound(Str), ..err],
		)
		expect_n = |Cmd.(cmd), decoder| Cmd.(
			cmd.with_decode(|r| r.decode(decoder)),
		)

		expect_1 : Cmd(Result, []),
		Result.Decode(a, [EmptyResult, ..err]) ->
			Cmd(a, [EmptyResult, FieldNotFound(Str), ..err])
		expect_1 = |Cmd.(cmd), decoder| Cmd.(
			cmd.with_limit(1).with_decode(
				|cmd_result| match cmd_result.decode(decoder)? {
					[row] => Ok(row)
					_ => Err(EmptyResult)
				},
			),
		)

		map : Cmd(_, _), (_ -> _) -> Cmd(_, _)
		map = |Cmd.(cmd), f| Cmd.(cmd.map(f))

		with_custom_decode : Cmd(_, _), (Result -> Try(_, _)) -> Cmd(_, _)
		with_custom_decode = |Cmd.(cmd), f| Cmd.(cmd.with_decode(f))

		to_inspect : Cmd(a, err) -> Str
		to_inspect = |Cmd.(cmd)| {
			{ kind, bindings, limit: _ } = cmd.params()
			kind_str = inspect_kind(kind)
			bindings_str = bindings.map_with_index(
				|val, index| {
					n = index + 1
					"$${n.to_str()} = ${inspect_binding(val)}"
				},
			)->Str.join_with("\n")

			"${kind_str}\n${bindings_str}"
		}

		Binding :: InternalCmd.Binding

		bind : Cmd(a, err), List(Binding) -> Cmd(a, err)
		bind = |Cmd.(cmd), bindings| {
			Cmd.(cmd.bind(bindings.map(|Binding.(b)| b)))
		}

		null : Binding
		null = Binding.(Null)

		str : Str -> Binding
		str = |value| Binding.(Text(value))

		u8 : U8 -> Binding
		u8 = |num| Binding.(Text(num.to_str()))

		u16 : U16 -> Binding
		u16 = |num| Binding.(Text(num.to_str()))

		u32 : U32 -> Binding
		u32 = |num| Binding.(Text(num.to_str()))

		u64 : U64 -> Binding
		u64 = |num| Binding.(Text(num.to_str()))

		u128 : U128 -> Binding
		u128 = |num| Binding.(Text(num.to_str()))

		i8 : I8 -> Binding
		i8 = |num| Binding.(Text(num.to_str()))

		i16 : I16 -> Binding
		i16 = |num| Binding.(Text(num.to_str()))

		i32 : I32 -> Binding
		i32 = |num| Binding.(Text(num.to_str()))

		i64 : I64 -> Binding
		i64 = |num| Binding.(Text(num.to_str()))

		i128 : I128 -> Binding
		i128 = |num| Binding.(Text(num.to_str()))

		f32 : F32 -> Binding
		f32 = |num| Binding.(Text(num.to_str()))

		f64 : F64 -> Binding
		f64 = |num| Binding.(Text(num.to_str()))

		bool : Bool -> Binding
		bool = |value| Binding.(Binary([if value 1 else 0]))

		bytes : List(U8) -> Binding
		bytes = |bytes| Binding.(Binary(bytes))
	}

	Batch(a, err) :: InternalBatch(a, err).{
		succeed : a -> Batch(a, err)
		succeed = |value| Batch.(InternalBatch.succeed(value))

		with_cmd : Batch((a -> b), err), Cmd(a, err) -> Batch(b, err)
		with_cmd = |Batch.(batch), Cmd.(cmd)|
			Batch.(InternalBatch.with_cmd(batch, cmd))

		sequence : List(Cmd(a, err)) -> Batch(List(a), err)
		sequence = |cmds| Batch.(
			InternalBatch.sequence(cmds.map(|Cmd.(cmd)| cmd)),
		)
	}

	Client :: [].{}

	Result : InternalResult
}

inspect_kind = |kind| match kind {
	SqlCmd(sql) => "SQL: ${sql}"
	PreparedCmd(prep) => "Prepared: ${prep.name}"
}

inspect_binding = |binding| match binding {
	Null => "NULL"
	Text(text) => text
	Binary(bin) => bin.map(|b| b.to_str())->Str.join_with(",")
}
