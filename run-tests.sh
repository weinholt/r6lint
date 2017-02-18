#!/bin/bash
set -ex

tests/test-reader.sps

bin/r6lint bin/r6lint
bin/r6lint tests/test-reader.sps
