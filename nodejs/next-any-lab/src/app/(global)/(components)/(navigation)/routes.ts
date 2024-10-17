import {NavItem} from '@/app/(global)/(components)/(navigation)/nav-wrap'

export const NAV_ITEMS: Array<NavItem> = [
  {
    label: 'fetch',
    href: '#',
    children: [
      {
        label: 'cache',
        subLabel: 'fetch-cache-test',
        href: 'fetch-test/cache',
      },
    ],
  },
  {
    label: 'layout',
    href: '#',
    children: [
      {
        label: 'grid',
        subLabel: 'grid over flow scroll test',
        href: '/layout/grid/scroll',
      }
    ]
  }
]
