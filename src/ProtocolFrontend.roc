import Bytes exposing [Encode]

ProtocolFrontend :: [].{

	FormatCode : [Text, Binary]

	startup : { user : Str, database : Str } -> List(U8)
	startup = |{ user, database }| Encode.sequence(
		[
			# Version number
			Encode.i16(3),
			Encode.i16(0),
			# Encoding
			Encode.sequence(
				[
					param("client_encoding", "utf_8"),
					param("user", user),
					param("database", database),
				],
			)->Encode.null_terminate(),
		],
	)->prepend_length()

	password_message : Str -> List(U8)
	password_message = |pwd| message('p', [Encode.c_str(pwd)])

	terminate : List(U8)
	terminate = message('X', [])

	parse : {
		sql : Str,
		name : Str, # optional
		param_type_ids : List(I32), # optional
	} -> List(U8)

	bind :
		{
			portal : Str, # Optional default ""
			prepared_statement : Str, # optional default ""
			format_codes : List(FormatCode), # optional
			param_values : List([Null, Value(List(U8))]),
			column_format_codes : List(FormatCode), # optional
		} -> List(U8)

	# describe_portal : { name ?? Str } -> List U8

	# describe_statement : { name ?? Str } -> List U8

	# execute : { portal ?? Str, limit ?? [None, Limit I32] } -> List U8

	# close_statement : { name : Str } -> List U8

	sync : List(U8)
	sync = message('S', [])

}

param : Str, Str -> List(U8)
param = |key, value| Encode.sequence(
	[
		Encode.c_str(key),
		Encode.c_str(value),
	],
)

format_code : FormatCode -> List(U8)
format_code = |code| match code {
	Text => Encode.i16(0)
	Binary => Encode.i16(1)
}

array : List(item), (item -> List(U8)) -> List(U8)
array = |items, item_encode| Encode.sequence(
	[
		Encode.i16(items.len().to_i16_wrap()),
		Encode.sequence(items.map(item_encode)),
	],
)

bytes : List(U8) -> List(U8)
bytes = |value| Encode.sequence(
	[
		Encode.i32(value.len().to_i32_wrap()),
		value,
	],
)

message : U8, List((List(U8))) -> List(U8)
message = |msg_type, content| Encode.sequence(
	[
		Encode.u8(msg_type),
		prepend_length(Encode.sequence(content)),
	],
)

prepend_length : List(U8) -> List(U8)
prepend_length = |msg| {
	total_length = msg.len().to_i32_wrap() + 4
	Encode.i32(total_length).concat(msg)
}
