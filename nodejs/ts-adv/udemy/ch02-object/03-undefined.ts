type ProductCh2 = {
  title: string
  tags?: string[]
}

type ProductCh2_2 = {
  title: string
  tags: string[] | undefined
}

const p1: ProductCh2 = { title: 'abc' }

// const p2: ProductCh2_2 = {title: 'abd'} // not working
// ts checks "all keys are set"
const p2: ProductCh2_2 = { title: 'abc', tags: undefined }

// so if you want to create optional property in type,
// use '?' instead ' | undefined' is more convenience.
