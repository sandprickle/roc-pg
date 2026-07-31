import Protocol
import Result

FormatCode : Protocol.Frontend.FormatCode

RowField : Protocol.Backend.RowField

ParameterField : Protocol.Backend.ParameterField

Limit : [None, Limit(I32)]

Binding : [
	Null,
	Text(Str),
	Binary(List(U8)),
]

Kind(k) : [
	SqlCmd(Str),
	PreparedCmd(
		{
			name : Str,
			fields : List(RowField),
			parameters : List(ParameterField),

		},
	),
	..k,
]

Params(p, k) : {
	kind : Kind(k),
	bindings : List(Binding),
	limit : Limit,
	..p,
}

Cmd(a, err) :: Params({ decode : Result -> Try(a, err) }, []).{

	Params(p, k) : Params(p, k)

	Kind(k) : Kind(k)

	Binding : Binding

	from_sql : Str -> Cmd(Result, [])
	from_sql = |sql| new(SqlCmd(sql))

	prepared : {
		name : Str,
		fields : List(RowField),
		parameters : List(ParameterField),
	} -> Cmd(Result, [])
	prepared = |prep| new(PreparedCmd(prep))

	params : Cmd(a, err) -> Params({}, [])
	params = |Cmd.(cmd)| {
		kind: cmd.kind,
		bindings: cmd.bindings,
		limit: cmd.limit,
	}

	with_limit : Cmd(a, err), I32 -> Cmd(a, err)
	with_limit = |Cmd.(cmd), limit| Cmd.({ ..cmd, limit: Limit(limit) })

	decode : Result, Cmd(a, err) -> Try(a, err)
	decode = |result, Cmd.(cmd)| (cmd.decode)(result)

	with_decode : Cmd(a, err_a), (Result -> Try(b, err_b)) -> Cmd(b, err_b)
	with_decode = |Cmd.(cmd), fn| Cmd.(
		{
			kind: cmd.kind,
			limit: cmd.limit,
			bindings: cmd.bindings,
			decode: fn,
		},
	)

	map : Cmd(a, err), (a -> b) -> Cmd(b, err)
	map = |Cmd.(cmd), fn| Cmd.(
		{
			kind: cmd.kind,
			limit: cmd.limit,
			bindings: cmd.bindings,
			decode: |res| (cmd.decode)(res).map_ok(fn),
		},
	)

	bind : Cmd(a, err), List(Binding) -> Cmd(a, err)
	bind = |Cmd.(cmd), bindings| Cmd.({ ..cmd, bindings })

	encode_bindings : List(Binding) -> {
		format_codes : List(FormatCode),
		param_values : List([Null, Value(List(U8))]),
	}
	encode_bindings = |bindings| {
		count = bindings.len()

		empty = {
			format_codes: List.with_capacity(count),
			param_values: List.with_capacity(count),
		}

		bindings.fold(
			empty,
			|state, binding| {
				{ format, value } = encode_single(binding)
				{
					format_codes: state.format_codes.append(format),
					param_values: state.param_values.append(value),
				}
			},
		)
	}
}

new : Kind([]) -> Cmd(Result, [])
new = |kind| Cmd.(
	{
		kind,
		limit: None,
		bindings: [],
		decode: |x| Ok(x),
	},
)

encode_single = |binding| match binding {
	Null => { value: Null, format: Binary }
	Binary(value) => { value: Value(value), format: Binary }
	Text(value) => { value: Value(value.to_utf8()), format: Text }
}
