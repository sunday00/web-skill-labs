import { Field, HStack, RadioGroup } from '@chakra-ui/react'

const Radio = ({
  title,
  name,
  defaultValue,
  items,
}: {
  title: string
  name: string
  defaultValue: string
  items: { value: string; label: string }[]
}) => {
  return (
    <Field.Root>
      <Field.Label>{title}</Field.Label>
      <RadioGroup.Root defaultValue={defaultValue} name={name}>
        <HStack gap="6">
          {items.map((item) => (
            <RadioGroup.Item value={item.value} key={item.value}>
              <RadioGroup.ItemHiddenInput />
              <RadioGroup.ItemIndicator />
              <RadioGroup.ItemText>{item.label}</RadioGroup.ItemText>
            </RadioGroup.Item>
          ))}
        </HStack>
      </RadioGroup.Root>
    </Field.Root>
  )
}

export default Radio
