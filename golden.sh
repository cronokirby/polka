#! /bin/sh
for file in tests/*.c; do
    echo "Testing $file"
    expected=`grep -oP "SHOULDBE \K\w+" $file | head -1`
    java -jar target/scala-0.18/polka-assembly-0.1.0.jar $file a.s
    gcc a.s
    ./a.out
    res="$?"
    if [ $res == $expected ]; then
        echo -e "\033[0;32mOK\033[0m"
    else
        echo -e "\033[0;31mexpected: $expected, got: $res\033[0m"
    fi
done
