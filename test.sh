if [ -z "$1" ]; then
    echo "Usage: test <session #>"
    return 1
fi

if [ "$1" -eq 1 ]; then
    echo "No tests for session 1."
    return 1
elif [ "$1" -eq 2 ]; then
    echo "No tests for session 2."
    return 1
fi

runhaskell -i"session$1" "session$1/test.hs"
return 0