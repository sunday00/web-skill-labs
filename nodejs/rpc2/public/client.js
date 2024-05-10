async function rpcClient(details) {
  const response = await fetch("http://localhost:4444/rpc", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(details),
  })

  return response.json()
}

