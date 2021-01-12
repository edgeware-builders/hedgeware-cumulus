use hex::FromHex;
use serde_json::Result;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use serde::{Serialize, Deserialize};
use rococo_parachain_primitives::*;

#[derive(Serialize, Deserialize, Debug)]
pub struct Allocation {
	pub balances: Vec<(AccountId, Balance)>,
	pub vesting: Vec<(AccountId, BlockNumber, BlockNumber, Balance)>,
}


#[derive(Serialize, Deserialize, Debug)]
pub struct AllocationRaw {
	balances: Vec<(String, String)>,
	vesting: Vec<(String, BlockNumber, BlockNumber, String)>,
}

pub fn get_lockdrop_participants_allocation() -> Result<AllocationRaw> {
	let path = Path::new("rococo-parachains/lockdrop/allocation/dump.json");
	let mut file = File::open(&path).unwrap();
	let mut data = String::new();
	file.read_to_string(&mut data).unwrap();
	let a: AllocationRaw = serde_json::from_str(&data)?;
	return Ok(a);
}

pub fn parse_allocation() -> Result<Allocation> {
	let allocation = get_lockdrop_participants_allocation().unwrap();
	let balances = allocation
		.balances
		.iter()
		.map(|b| {
			let balance = b.1.to_string().parse::<Balance>().unwrap();
			return (<[u8; 32]>::from_hex(b.0.clone()).unwrap().into(), balance);
		})
		.filter(|b| b.1 > 0)
		.collect();
	let vesting = allocation
		.vesting
		.iter()
		.map(|b| {
			let vesting_balance = b.3.to_string().parse::<Balance>().unwrap();
			return (
				(<[u8; 32]>::from_hex(b.0.clone()).unwrap()).into(),
				b.1,
				b.2,
				vesting_balance,
			);
		})
		.filter(|b| b.3 > 0)
		.collect();

	Ok(Allocation {
		balances,
		vesting,
	})
}

#[cfg(test)]
mod tests {
	#[test]
	fn it_works() {
		assert_eq!(2 + 2, 4);
	}
}
