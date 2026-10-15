import { describe, it, expect } from 'vite-plus/test'
import { sum } from './sum.ts'

describe('sum', () => {
  it('should return sum of two num', () => {
    expect(sum(2, 3)).toBe(5)
  })
})
