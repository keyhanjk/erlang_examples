program=$1
programFile="$program.erl"
output=`eval 'erlc $programFile'`
func="hello"
if [ -z "$output" ]
then
    # printf "output:\n"
    rm *.beam
    rm *.dump
    erl -noshell -s $program start -s $func stop
else
    printf "error:\n"
    printf "$output\n\n"
fi