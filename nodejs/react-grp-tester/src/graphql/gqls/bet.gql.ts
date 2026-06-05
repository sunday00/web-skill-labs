import { gql } from '@apollo/client'

export const getBetById = gql`
  query idBet($input: FindById!) {
    idBet(input: $input) {
      id
      category
      date
      price
      user {
        id
        nickname
      }
    }
  }
`

export const getBetsBetweenDates = gql`
  query daysBetweenBets($input: BetweenDates!) {
    daysBetweenBets(input: $input) {
      id
      category
      date
      price
      user {
        id
        nickname
      }
    }
  }
`
