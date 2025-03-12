export enum UserRole {
  GUEST,
  MEMBER,
  INSTRUCTOR,
  MANAGER,
  ADMIN,
}

export type User = {
  id: string
  password: string
  email: string
  name: string
  status: number
  role: UserRole
  point: number
  createdAt: string
}
