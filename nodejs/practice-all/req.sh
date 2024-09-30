CONTROLLER=$1
METHOD=$2
ARGUMENT=$3

echo "controller: ${CONTROLLER}"
echo "method: ${METHOD}"
echo "argument: ${ARGUMENT}"

curl -X GET "http://localhost:3033/${CONTROLLER}/${METHOD}?argument=${ARGUMENT}"

echo 
