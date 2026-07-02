Encode := [].{
	sequence : List(List(U8)) -> List(U8)
	sequence = |list| list.fold([], |acc, bytes| acc.concat(bytes))

	# Unsigned integers

	u8 : U8 -> List(U8)
	u8 = |value| List.single(value)

	u16 : U16 -> List(U8)
	u16 = |value| sized(value, 16)

	u32 : U32 -> List(U8)
	u32 = |value| sized(value, 32)

	u64 : U64 -> List(U8)
	u64 = |value| sized(value, 64)

	# Signed integers

	i8 : I8 -> List(U8)
	i8 = |value| List.single(value.to_u8_wrap())

	i16 : I16 -> List(U8)
	i16 = |value| sized(value, 16)

	i32 : I32 -> List(U8)
	i32 = |value| sized(value, 32)

	i64 : I64 -> List(U8)
	i64 = |value| sized(value, 64)

	# Strings

	c_str : Str -> List(U8)
	c_str = |value| value.to_utf8()->null_terminate()

	null_terminate : List(U8) -> List(U8)
	null_terminate = |bytes| bytes.append(0)
}

sized : int, U8 -> List(U8)
	where [
		int.shift_right_by : int, U8 -> int,
		int.to_u8_wrap : int -> U8,
	]
sized = |value, size| sized_help(value, (size - 8), [])

sized_help : int, U8, List(U8) -> List(U8)
	where [
		int.shift_right_by : int, U8 -> int,
		int.to_u8_wrap : int -> U8,
	]
sized_help = |value, offset, collected| {
	part = value.shift_right_by(offset).to_u8_wrap()
	added = collected.append(part)

	if offset == 0 {
		added
	} else {
		sized_help(value, (offset - 8), added)
	}
}
