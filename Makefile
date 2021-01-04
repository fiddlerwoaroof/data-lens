test:
	docker build . --tag data-lens-transducer-test
	docker run data-lens-transducer-test --eval "(asdf:test-system :data-lens/beta/transducers)" --quit
	docker image rm --force data-lens-transducer-test:latest

