/*
  YOUR JOB:
  1. Make sure Allowed_Addresses shows error if not initialized. 
  Hint: Look at the compiler configurations.
  2. After you  get the error on line 8, try to address it properly.
*/

export class Server {
  Allowed_Addresses: string[] = []

  constructor(allow_address: boolean, address: string[]) {
    // allow_address ? (this.Allowed_Addresses = address) : (this.Allowed_Addresses = [])
    if (allow_address) {
      this.Allowed_Addresses = address
    }
  }
}

const server = new Server(false, [])

server.Allowed_Addresses.filter((address) => address.charAt(4) === 's')
