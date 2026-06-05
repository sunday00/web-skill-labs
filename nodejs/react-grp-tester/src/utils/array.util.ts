export const unique = <T>(a: T[]): T[] => {
  return Array.from(new Set(a))
}

export const group = <T>(a: T[], key: string): T[][] => {
  const keys = key.split('.')
  const rRaw: { [p: string]: T[] } = {}

  a.forEach((item) => {
    let i: { [p: string]: unknown } | string = item as { [p: string]: unknown }
    for (const k of keys) {
      i = i[k] as { [p: string]: unknown }
    }

    const p = i as unknown as string

    if (rRaw[p]) rRaw[p].push(item)
    else rRaw[p] = [item]
  })

  return Object.values(rRaw)
}
