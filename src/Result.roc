import ProtocolBackend

Result :: {
	fields : List(RowField),
	rows : List(List(List(U8))),
	parameters : List(ParameterField),
}.{
	RowField : ProtocolBackend.RowField
	ParameterField : ProtocolBackend.ParameterField

	create = |x| Result.(x)

	fields : Result -> List(RowField)
	fields = |Result.(result)| result.fields

	rows : Result -> List(List(List(U8)))
	rows = |Result.(result)| result.rows

	len : Result -> U64
	len = |Result.(result)| result.rows.len()

	Decode(a, err) :: List(RowField) -> Try(
		# TODO: should have `..err` formatter removes it
		(List(List(U8)) -> Try(a, [FieldNotFound(Str), ..err])),
		[FieldNotFound(Str)],
	)

	decode : Result, Decode(a, err) -> Try(List(a), [FieldNotFound(Str), ..err])
	decode = |Result.(r), Decode.(get_decode)|
		match get_decode(r.fields) {
			Ok(fn) => map_try(r.rows, fn)
			Err(FieldNotFound(name)) => Err(FieldNotFound(name))
		}

	str = decoder(|s| Ok(s))

	u8 = decoder(U8.from_str)

	u16 = decoder(U16.from_str)

	u32 = decoder(U32.from_str)

	u64 = decoder(U64.from_str)

	u128 = decoder(U128.from_str)

	i8 = decoder(I8.from_str)

	i16 = decoder(I16.from_str)

	i32 = decoder(I32.from_str)

	i64 = decoder(I64.from_str)

	i128 = decoder(I128.from_str)

	f32 = decoder(F32.from_str)

	f64 = decoder(F64.from_str)

	dec = decoder(Dec.from_str)

	bool = decoder(
		|v| match v {
			"t" => Ok(Bool.True)
			"f" => Ok(Bool.False)
			_ => Err(InvalidBoolStr)
		},
	)

	succeed = |value|
		Decode.(|_| Ok(|_| Ok(value)))

	result_with = |a, b| map2(a, b, |fn, val| fn(val))

	apply = |a| |fn| result_with(fn, a)

	## Use with Roc's [Record Builder](https://www.roc-lang.org/tutorial#record-builder)
	## syntax to build records of your returned rows:
	##
	## ```
	## Pg.Cmd.expect_n(
	##     { Pg.Result.combine <-
	##         name: Pg.Result.str("name"),
	##         age: Pg.Result.u8("age"),
	##     },
	## )
	## ```
	# NOTE: `combine` is an alias of `map2` simply to increase its
	# discoverability and user-friendliness for its intended use-case.
	combine = map2
}

map_try : List(a), (a -> Try(b, err)) -> Try(List(b), err)
map_try = |input, fn| {
	var $mapped = []
	for x in input {
		match fn(x) {
			Ok(y) => {
				$mapped = $mapped.append(y)
			}
			Err(err) => return Err(err)
		}
	}
	Ok($mapped)
}

try : Try(a, err), (a -> Try(b, err)) -> Try(b, err)
try = |try_a, fn| {
	match try_a {
		Ok(a) => fn(a)
		Err(err) => Err(err)
	}
}

decoder = |fn| |name| Decode.(
	|row_fields|
		match row_fields.find_first_index(|f| f.name == name) {
			Ok(index) => Ok(
				|row| match row.get(index) {
					Ok(bytes) => {
						str_value = Str.from_utf8(bytes)?
						fn(str_value)
					}
					Err(OutOfBounds) => Err(FieldNotFound(name))
				},
			)

			Err(NotFound) => Err(FieldNotFound(name))
		},
)

map2 = |Decode.(a), Decode.(b), cb| Decode.(
	|row_fields| try(
		a(row_fields),
		|decode_a| try(
			b(row_fields),
			|decode_b| Ok(
				|row| try(
					decode_a(row),
					|value_a| try(
						decode_b(row),
						|value_b| Ok(cb(value_a, value_b)),
					),
				),
			),
		),
	),
)
