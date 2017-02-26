#!/bin/bash
set -ex

function cleanup {
  rm -f r6lintrc-test
}
trap cleanup EXIT


scheme --compile-imported-libraries --program bin/r6lint || true

tests/test-reader.sps
tests/test-linter.sps

cat > r6lintrc-test <<EOF
((library-path . ("$PWD/..")))
EOF

bin/r6lint --config r6lintrc-test bin/r6lint
bin/r6lint -c r6lintrc-test tests/test-reader.sps
bin/r6lint -c r6lintrc-test tests/test-linter.sps
echo All tests passed
