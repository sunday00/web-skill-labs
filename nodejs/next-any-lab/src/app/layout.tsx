// import type { Metadata } from 'next'
import React, { ReactNode } from 'react'
import { Providers } from '@/app/providers'
import { theme } from '@chakra-ui/react'
import { NavWrap } from './(global)/(components)/(navigation)/nav-wrap'

export default function RootLayout({
  children,
}: Readonly<{
  children: ReactNode
}>) {
  return (
    <html lang="ko">
      <body suppressHydrationWarning={true}>
        {/* <Providers> */}
        <NavWrap />
        <main
          style={{
            padding: theme.space['4'],
          }}
        >
          {children}
        </main>
        {/* </Providers> */}
      </body>
    </html>
  )
}
