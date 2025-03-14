export const generateRandomKey = () =>
  (Math.random() + 1).toString(36).substring(7) + (Math.random() + 1).toString(36).substring(7)
