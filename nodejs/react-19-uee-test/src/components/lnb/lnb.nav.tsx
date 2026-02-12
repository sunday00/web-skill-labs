const LnbNav = ({
  items,
}: {
  items: { id: string; label: string; href: string }[]
}) => {
  return (
    <ul>
      {items.map((item) => (
        <li key={item.id}>
          <a href={item.href}>{item.label}</a>
        </li>
      ))}
    </ul>
  )
}

export default LnbNav
