export interface UserData {
  id: number
  name: string
  email: string
  age: number
  preferences?: {
    theme: 'light' | 'dark'
    notifications: boolean
  }
}

export function processUserData(userData: UserData) : string {
  const userSummary = `
    User: ${userData.name} (ID: ${userData.id})
    Email: ${userData.email}
    Age: ${userData.age}
    Theme: ${userData.preferences?.theme || 'default'}
    Notifications: ${userData.preferences?.notifications ? 'Enabled' : 'Disabled'} 
  `

  return userSummary.trim()
}

const userDataFromApi: UserData = {
  id: "1234",
  name: 'Jane doe',
  email: "invalid-email",
  age: "10"
}

const r = processUserData(userDataFromApi)

console.log(r)