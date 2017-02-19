#!/bin/bash
set -ex

tests/test-reader.sps
tests/test-linter.sps

bin/r6lint bin/r6lint
bin/r6lint tests/test-reader.sps
