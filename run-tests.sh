#!/bin/bash
set -ex

scheme --compile-imported-libraries --program bin/r6lint || true

tests/test-reader.sps
tests/test-linter.sps

bin/r6lint bin/r6lint
bin/r6lint tests/test-reader.sps
bin/r6lint tests/test-linter.sps
