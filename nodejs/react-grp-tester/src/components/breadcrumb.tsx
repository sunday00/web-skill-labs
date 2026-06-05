import { Breadcrumb, For, Icon } from '@chakra-ui/react'
import { LuHouse } from 'react-icons/lu'
import type { IconType } from 'react-icons'
import { Fragment, useState } from 'react'

export type BreadcrumbItem = {
  href: string
  label: string
  icon?: IconType
}

export type BreadcrumbProps = {
  items: BreadcrumbItem[]
}

const BreadcrumbWrap = ({ items }: BreadcrumbProps) => {
  const current = items[items.length - 1]
  const [i, _] = useState(items.slice(0, items.length - 1))

  const homeHref = import.meta.env.VITE_APP_HOST ?? 'http://localhost:3000'

  return (
    <Breadcrumb.Root size={'md'} mb={6}>
      <Breadcrumb.List>
        <Breadcrumb.Item>
          <Breadcrumb.Link href={homeHref}>
            <LuHouse />
            Home
          </Breadcrumb.Link>
        </Breadcrumb.Item>

        <Breadcrumb.Separator />

        <For each={i}>
          {(item: BreadcrumbItem) => {
            return (
              <Fragment key={item.label}>
                <Breadcrumb.Item>
                  <Breadcrumb.Link href={item.href}>
                    <Icon as={item.icon} />
                    {item.label}
                  </Breadcrumb.Link>
                </Breadcrumb.Item>

                <Breadcrumb.Separator />
              </Fragment>
            )
          }}
        </For>

        <Breadcrumb.Item>
          <Breadcrumb.CurrentLink>{current?.label}</Breadcrumb.CurrentLink>
        </Breadcrumb.Item>
      </Breadcrumb.List>
    </Breadcrumb.Root>
  )
}

export default BreadcrumbWrap
