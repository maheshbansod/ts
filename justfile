
default:
	just --list

test-all:
	cargo test
	cargo test --features ts
