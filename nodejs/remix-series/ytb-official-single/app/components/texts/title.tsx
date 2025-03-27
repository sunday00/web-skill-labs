import { ReactNode } from 'react'

export default function Title({
  children,
  text,
  as = 2,
  className,
  bold = true,
}: {
  as?: number
  text: string
  children?: ReactNode
  className?: string
  bold?: boolean
}) {
  const combinedClassname = ` ${bold ? 'font-bold' : 'font-normal'} ${className}`
  const combinedContent = children ? (
    <>
      {children} <span>{text}</span>
    </>
  ) : (
    <>{text}</>
  )

  switch (as) {
    case 1:
      return <h1 className={`text-6xl ${combinedClassname}`}>{combinedContent}</h1>
    case 2:
    default:
      return <h2 className={`text-4xl ${combinedClassname}`}>{combinedContent}</h2>
    case 3:
      return <h3 className={`text-2xl ${combinedClassname}`}>{combinedContent}</h3>
    case 4:
      return <h4 className={`text-xl ${combinedClassname}`}>{combinedContent}</h4>
    case 5:
      return <h5 className={`text-lg ${combinedClassname}`}>{combinedContent}</h5>
    case 6:
      return <h6 className={`text-sm ${combinedClassname}`}>{combinedContent}</h6>
  }
}
