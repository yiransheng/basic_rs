scriptName=$(basename "$0")

usage() {
  echo -n "${scriptName} [closure jar] [input js]

Runs Google Closure Compiler on compiled js, with some preset flags.
"
}

[[ $# -eq 0 ]] && set -- "--help"

while [[ $1 = -?* ]]; do
  case $1 in
    -h|--help) usage >&2; exit ;;
    *) die "invalid option: '$1'." ;;
  esac
  shift
done

if [ $# -lt 2 ]
then
  usage >&2
  exit
fi

echo "function WordWidth() {}" > temp.js

java -jar $1 --js $2 --js_output_file $2.optimized.js \
    --language_in ECMASCRIPT_2017 \
    --language_out ECMASCRIPT_2017 \
    --externs temp.js \
    --compilation_level ADVANCED

rm temp.js
