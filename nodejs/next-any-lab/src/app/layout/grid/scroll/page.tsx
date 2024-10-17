import {Box, Grid, GridItem, theme} from '@chakra-ui/react'

const Child = () => {
  return (<>
    <Box  bg={theme.colors.yellow['400']} whiteSpace={'nowrap'}>
      <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit. Ab ad architecto assumenda </p>
      <p>consequatur dicta, dolore dolorum id illum ipsum laboriosam laborum odio odit pariatur porro repudiandae sit vitae? Ex, ratione?</p>
    </Box>
  </>)
}

const LayoutGridScrollPage = () => {
  return (
    <Box bg={theme.colors.gray['400']} p={theme.space['6']}>
      <Grid templateColumns={'repeat(2, minmax(50%, 50%))'} gap={theme.space['4']} boxSize={'fit-content'}>
        <GridItem bg={theme.colors.white} p={theme.space['2']} colSpan={1} >
          <Box  overflowX={'scroll'}>
            <Child></Child>
          </Box>

        </GridItem>
        <GridItem bg={theme.colors.white} p={theme.space['2']} colSpan={1}>
          fdsfdsfds
        </GridItem>
      </Grid>
    </Box>
  )
}

 export default LayoutGridScrollPage