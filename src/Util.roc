Util :: [].{

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
}
