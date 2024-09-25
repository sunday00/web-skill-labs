'use client'

import {
  Box,
  Text,
  Icon,
  Flex,
  Popover,
  PopoverContent,
  PopoverTrigger,
  Stack,
  theme,
  useColorModeValue,
} from '@chakra-ui/react'
import { NAV_ITEMS } from './routes'
import Link from 'next/link'
import { ChevronRightIcon } from '@chakra-ui/icons'

export interface NavItem {
  label: string
  subLabel?: string
  children?: Array<NavItem>
  href?: string
}

const SubNav = ({ label, href, subLabel }: NavItem) => {
  return (
    <Box
      as={Link}
      href={href}
      role={'group'}
      display={'block'}
      p={2}
      rounded={'md'}
      _hover={{ bg: useColorModeValue('pink.50', 'gray.900') }}
    >
      <Stack direction={'row'} align={'center'}>
        <Box>
          <Text
            transition={'all .3s ease'}
            _groupHover={{ color: 'pink.400' }}
            fontWeight={500}
            fontSize={'x-large'}
          >
            {label}
          </Text>
          <Text fontSize={'sm'}>{subLabel}</Text>
        </Box>
        <Flex
          transition={'all .3s ease'}
          transform={'translateX(-10px)'}
          opacity={0}
          _groupHover={{ opacity: '100%', transform: 'translateX(0)' }}
          justify={'flex-end'}
          align={'center'}
          flex={1}
        >
          <Icon color={'pink.400'} w={5} h={5} as={ChevronRightIcon} />
        </Flex>
      </Stack>
    </Box>
  )
}

const NavWrap = () => {
  return (
    <Flex justifyContent={'space-between'} p={theme.space['10']}>
      <Box className="logo">
        <a href="/">home</a>
      </Box>
      <Box as={'nav'} display={'flex'} gap={theme.space['2']}>
        <Stack
          as={'li'}
          direction={'row'}
          spacing={4}
          className={'nav-item'}
          aria-label={'nav-item'}
        >
          {NAV_ITEMS.map((navItem) => (
            <Box key={navItem.label}>
              <Popover trigger={'hover'} placement={'bottom-start'}>
                <PopoverTrigger>
                  <Box
                    as={Link}
                    p={2}
                    href={navItem.href ?? '#'}
                    fontSize={'sm'}
                    fontWeight={500}
                    _hover={{
                      textDecoration: 'none',
                    }}
                  >
                    {navItem.label}
                  </Box>
                </PopoverTrigger>

                {navItem.children && (
                  <PopoverContent
                    border={0}
                    boxShadow={'xl'}
                    p={4}
                    rounded={'xl'}
                    minW={'sm'}
                    borderRadius={theme.radii['md']}
                    bg={theme.colors.gray['50']}
                  >
                    <Stack>
                      {navItem.children.map((child: NavItem) => (
                        <SubNav key={child.label} {...child} />
                      ))}
                    </Stack>
                  </PopoverContent>
                )}
              </Popover>
            </Box>
          ))}
        </Stack>
      </Box>
    </Flex>
  )
}

export { NavWrap }
