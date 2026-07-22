import Bytes exposing [Decode]

ProtocolBackend := [].{

	Error : {
		localized_severity : Str,
		severity : Try(ErrorSeverity, {}),
		code : Str,
		message : Str,
		detail : Try(Str, {}),
		hint : Try(Str, {}),
		position : Try(U32, {}),
		internal_position : Try(U32, {}),
		internal_query : Try(Str, {}),
		ewhere : Try(Str, {}),
		schema_name : Try(Str, {}),
		table_name : Try(Str, {}),
		column_name : Try(Str, {}),
		data_type_name : Try(Str, {}),
		constraint_name : Try(Str, {}),
		file : Try(Str, {}),
		line : Try(Str, {}),
		routine : Try(Str, {}),
	}

	KeyData : { process_id : I32, secret_key : I32 }

	Message : [
		AuthOk,
		AuthCleartextPassword,
		AuthUnsupported,
		ParameterStatus({ name : Str, value : Str }),
		BackendKeyData(KeyData),
		ReadyForQuery(Status),
		ErrorResponse(Error),
		ParseComplete,
		BindComplete,
		NoticeResponse(List({ code : U8, value : Str })),
		NoData,
		RowDescription(List(RowField)),
		ParameterDescription(List(ParameterField)),
		DataRow(List(List(U8))),
		PortalSuspended,
		CommandComplete(Str),
		EmptyQueryResponse,
		CloseComplete,
	]

	ParameterField : {
		data_type_oid : I32,
	}

	RowField : {
		name : Str,
		column : Try({ table_oid : I32, attribute_number : I16 }, [NotAColumn]),
		data_type_oid : I32,
		data_type_size : I16,
		type_modifier : I32,
		format_code : I16,
	}

	Status : [Idle, TransactionBlock, FailedTransactionBlock]

	header : Decode({ msg_type : U8, len : I32 }, _)
	header = Decode.await(
		Decode.u8,
		|msg_type| Decode.map(
			Decode.i32,
			|len| { msg_type, len: len - 4 },
		),
	)

	message : U8 -> Decode(Message, _)
	message = |msg_type| match msg_type {
		'R' => auth_request
		'S' => param_status
		'K' => backend_key_data
		'Z' => ready_for_query
		'E' => error_response
		'1' => Decode.succeed(ParseComplete)
		'2' => Decode.succeed(BindComplete)
		'N' => notice_response
		'n' => Decode.succeed(NoData)
		'T' => row_description
		't' => parameter_description
		'D' => data_row
		's' => Decode.succeed(PortalSuspended)
		'C' => command_complete
		'I' => Decode.succeed(EmptyQueryResponse)
		'3' => Decode.succeed(CloseComplete)
		_ => Decode.fail(UnrecognizedBackendMessage(msg_type))
	}
}

backend_key_data : Decode(Message, _)
backend_key_data = Decode.await(
	Decode.i32,
	|process_id| Decode.await(
		Decode.i32,
		|secret_key|
			Decode.succeed(BackendKeyData({ process_id, secret_key })),
	),
)

ready_for_query : Decode(Message, _)
ready_for_query = Decode.await(
	Decode.u8,
	|status| match status {
		'I' => Decode.succeed(ReadyForQuery(Idle))
		'T' => Decode.succeed(ReadyForQuery(TransactionBlock))
		'E' => Decode.succeed(ReadyForQuery(FailedTransactionBlock))
		_ => Decode.fail(UnrecognizedBackendStatus(status))
	},
)

read_notice_responses : Decode(List({ code : U8, value : Str }), _)
read_notice_responses = Decode.loop(
	[],
	|collected|
		Decode.await(
			Decode.u8,
			|code|
				if code == 0
					Decode.succeed(Done(collected))
				else
					Decode.map(
						Decode.c_str,
						|value|
							Loop(List.append(collected, { code, value })),
					),
		),
)

notice_response = Decode.await(
	read_notice_responses,
	|notices|
		Decode.succeed(NoticeResponse(notices)),
)

# TODO
error_response : Decode(Message, _)
error_response = known_str_fields.await(
	|dict| 'S'->required_field(
		dict,
		|localized_severity| 'V'->optional_field_with(
			dict,
			decode_severity,
			|severity| 'C'->required_field(
				dict,
				|code| 'M'->required_field(
					dict,
					|msg| 'P'->optional_field_with(
						dict,
						U32.from_str,
						|position| 'p'->optional_field_with(
							dict,
							U32.from_str,
							|internal_position| ErrorResponse({
								localized_severity,
								severity,
								code,
								message: msg,
								detail: 'D'->optional_field(dict),
								hint: 'H'->optional_field(dict),
								position,
								internal_position,
								internal_query: 'q'->optional_field(dict),
								ewhere: 'W'->optional_field(dict),
								schema_name: 's'->optional_field(dict),
								table_name: 't'->optional_field(dict),
								column_name: 'c'->optional_field(dict),
								data_type_name: 'd'->optional_field(dict),
								constraint_name: 'n'->optional_field(dict),
								file: 'F'->optional_field(dict),
								line: 'L'->optional_field(dict),
								routine: 'R'->optional_field(dict),
							})->Decode.succeed(),
						),
					),
				),
			),
		),
	),
)

optional_field = |field_id, dict|
	match Dict.get(dict, field_id) {
		Ok(value) =>
			Ok(value)

		Err(_) =>
			Err({})
		}

optional_field_with = |field_id, dict, validate, callback| {
	result = Dict.get(dict, field_id)
	match result {
		Ok(value) => match validate(value) {
			Ok(validated) => callback(Ok(validated))
			Err(err) => Decode.fail(err)
		}
		Err(_) => callback(Err({}))
	}
}

required_field = |field_id, dict, callback| {
	result = Dict.get(dict, field_id)
	match result {
		Ok(value) => callback(value)
		Err(_) => Decode.fail(MissingField(field_id))
	}
}

ErrorSeverity : [
	Error,
	Fatal,
	Panic,
	Warning,
	Notice,
	Debug,
	Info,
	Log,
]

decode_severity : Str -> Try(ErrorSeverity, [InvalidSeverity(Str), ..])
decode_severity = |str|
	match str {
		"ERROR" => Ok(Error)
		"FATAL" => Ok(Fatal)
		"PANIC" => Ok(Panic)
		"WARNING" => Ok(Warning)
		"NOTICE" => Ok(Notice)
		"DEBUG" => Ok(Debug)
		"INFO" => Ok(Info)
		"LOG" => Ok(Log)
		_ => Err(InvalidSeverity(str))
	}

known_str_fields : Decode(Dict(U8, Str), _)
known_str_fields = Decode.loop(
	Dict.empty(),
	|collected| Decode.await(
		Decode.u8,
		|field_id| if field_id == 0 Decode.succeed(Done(collected)) else Decode.map(
			Decode.c_str,
			|value| collected->Dict.insert(field_id, value)->Loop,
		),
	),
)

parameter_description : Decode(Message, _)
parameter_description = Decode.await(
	Decode.i16,
	|field_count|
		if field_count == 0 Decode.succeed(ParameterDescription([]))
		else fixed_list(field_count, parameter_field)->Decode.map(
			|d| ParameterDescription(d),
		),
)

parameter_field : Decode(ParameterField, _)
parameter_field = Decode.await(
	Decode.i32,
	|data_type_oid| Decode.succeed({ data_type_oid: data_type_oid }),
)

row_description : Decode(Message, _)
row_description = 
	Decode.await(
		Decode.i16,
		|field_count|
			fixed_list(field_count, row_field)->Decode.map(|d| RowDescription(d)),
	)

row_field : Decode(RowField, _)
row_field = Decode.c_str.await(
	|name| Decode.i32.await(
		|table_oid| Decode.i16.await(
			|attribute_number| Decode.i32.await(
				|data_type_oid| Decode.i16.await(
					|data_type_size| Decode.i32.await(
						|type_modifier| Decode.i16.map(
							|format_code| {
								column = 
									if table_oid != 0 and attribute_number != 0
										Ok({ table_oid, attribute_number })
									else
										Err(NotAColumn)

								{
									name,
									column,
									data_type_oid,
									data_type_size,
									type_modifier,
									format_code,
								}
							},
						),
					),
				),
			),
		),
	),
)

data_row : Decode(Message, _)
data_row = Decode.await(
	Decode.i16,
	|column_count| fixed_list(
		column_count,
		Decode.await(
			Decode.i32,
			|value_len|
				if value_len == -1
					Decode.succeed([])
				else
					Decode.take(value_len.to_u64_wrap(), |x| x),
		),
	)->Decode.map(|r| DataRow(r)),
)

fixed_list = |count, item_decode| Decode.loop(
	List.with_capacity(count.to_u64_wrap()),
	|collected| Decode.map(
		item_decode,
		|item| {
			added = List.append(collected, item)

			if List.len(added) == (count.to_u64_wrap())
				Done(added)
			else
				Loop(added)
		},
	),
)

command_complete : Decode(Message, _)
command_complete = Decode.map(Decode.c_str, |str| CommandComplete(str))

Msg : []

auth_request : Decode(Message, _)
auth_request = Decode.map(
	Decode.i32,
	|auth_type| match auth_type {
		0 => AuthOk
		3 => AuthCleartextPassword
		_ => AuthUnsupported
	},
)

param_status : Decode(Message, _)
param_status = Decode.await(
	Decode.c_str,
	|name| Decode.await(
		Decode.c_str,
		|value|
			Decode.succeed(ParameterStatus({ name, value })),
	),
)
