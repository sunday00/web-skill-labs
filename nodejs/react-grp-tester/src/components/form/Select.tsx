import {
  Field,
  Input,
  type ListCollection,
  Portal,
  Select as ChSelect,
  Show,
} from '@chakra-ui/react'
import { type ChangeEvent, useState } from 'react'
import { EmptyValue } from '@/utils/constants.ts'

const Select = ({
  title,
  name,
  collection,
  hasCustom,
  exist,
}: {
  title: string
  name: string
  collection: ListCollection
  hasCustom?: boolean
  exist?: string | null
}) => {
  const [etcValue, setEtcValue] = useState<string>(EmptyValue)
  const [selected, setSelected] = useState<string | null>(null)

  let nCollection = collection

  if (hasCustom) {
    nCollection = collection.append({ label: 'etc', value: etcValue })
  }

  const onSelectedChange = (evt: ChangeEvent<HTMLInputElement>) => {
    if (evt.target.value === etcValue) {
      setSelected('etc')

      const st = setTimeout(() => {
        ;(document.querySelector('.etcInput') as HTMLInputElement)?.focus()

        clearTimeout(st)
      }, 100)
    } else {
      setSelected('!etc')
    }
  }

  const onEtcChange = (evt: ChangeEvent<HTMLInputElement>) => {
    nCollection.update(evt.target.value, nCollection.find(etcValue))
    setEtcValue(evt.target.value)
  }

  return (
    <Field.Root>
      <Field.Label>{title}</Field.Label>
      <ChSelect.Root
        collection={nCollection}
        size="sm"
        width="full"
        onChange={onSelectedChange}
        defaultValue={exist ? [exist] : []}
      >
        <ChSelect.HiddenSelect name={name} />
        <ChSelect.Label>Select {title}</ChSelect.Label>
        <ChSelect.Control>
          <ChSelect.Trigger>
            <ChSelect.ValueText placeholder={`Select ${title}`} />
          </ChSelect.Trigger>
          <ChSelect.IndicatorGroup>
            <ChSelect.Indicator />
          </ChSelect.IndicatorGroup>
        </ChSelect.Control>

        <Portal>
          <ChSelect.Positioner>
            <ChSelect.Content>
              {nCollection.items.map((item) => (
                <ChSelect.Item item={item} key={item.value}>
                  {item.label}
                  <ChSelect.ItemIndicator />
                </ChSelect.Item>
              ))}
            </ChSelect.Content>
          </ChSelect.Positioner>
        </Portal>
      </ChSelect.Root>

      <Show when={hasCustom && selected === 'etc'}>
        <Input type={'text'} onChange={onEtcChange} className={'etcInput'} />
      </Show>
    </Field.Root>
  )
}

export default Select
