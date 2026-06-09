import { useNavigate } from 'react-router'
import { useApolloClient } from '@apollo/client/react'
import { type FormEvent, useState } from 'react'
import { gql, type OperationVariables } from '@apollo/client'
import { gqlCommonContext } from '@/utils/gql.util.ts'

const DynamicCreate = () => {
  const navigate = useNavigate()

  const [loading, setLoading] = useState(false)

  const apollo = useApolloClient()

  const _presignedURL = async (
    files: HTMLInputElement[],
  ): Promise<{
    AttachMutation: {
      [k: string]: { item: { id: string; upload: string; sig: string } }
    }
  }> => {
    const presignedInputs: string[] = []
    const presignedVariables: NoInfer<OperationVariables> = {}

    const presignedQ = files
      .filter((f) => f.value.length > 0)
      .map((f, idx) => {
        presignedInputs.push(`$input${idx}: AttachItem!`)
        presignedVariables[`input${idx}`] = {
          originalExt: 'png',
          originalName: f.files?.[0].name,
          sortKey: idx + 1,
          type: 'IMAGE',
        }

        return `f${idx}: attachPresign(input: $input${idx}) {
          item {
            sortKey
            id
            upload
            sig
            cdn
          }
          success
        }
       `
      })

    const presignedQL = `
      mutation AttachPresign(
        ${presignedInputs.join('\n')}
      ) {
        AttachMutation {
          ${presignedQ.join('\n')}
        }
      }
    `

    const presignedUrl = await apollo.mutate({
      mutation: gql`
        ${presignedQL}
      `,
      variables: presignedVariables,
      context: gqlCommonContext(),
    })

    return presignedUrl.data as {
      AttachMutation: {
        [k: string]: { item: { id: string; upload: string; sig: string } }
      }
    }
  }

  const _uploadToAzure = async (
    file: {
      item: { id: string; upload: string; sig: string }
    },
    phyF: File,
  ) => {
    const url = `${file!.item.upload}/${file!.item.id}.png?${file!.item.sig}`

    const r = await fetch(url, {
      method: 'PUT',
      headers: {
        ContentType: 'image/png',
        'x-ms-blob-type': 'BlockBlob',
      },
      body: phyF,
    })

    return r.status === 200
  }

  const _uploadFinish = async (
    files: { id: string; upload: string; sig: string }[],
  ) => {
    const inputs: string[] = []
    const variables: NoInfer<OperationVariables> = {}

    const q = files.map((f, idx) => {
      inputs.push(`$input${idx}: AttachUploadedInput!`)
      variables[`input${idx}`] = {
        id: f.id,
        sizes: [
          {
            name: 'full',
            wh: [300, 300],
          },
        ],
      }

      return `f${idx}: attachUploaded(input: $input${idx}) {
          item 
          success
        }
       `
    })

    const ql = `
      mutation AttachUploaded(
        ${inputs.join('\n')}
      ) {
        AttachMutation {
          ${q.join('\n')}
        }
      }
    `

    console.log({
      mutation: `
        ${ql}
      `,
      variables: variables,
      context: gqlCommonContext(),
    })

    const res = await apollo.mutate({
      mutation: gql`
        ${ql}
      `,
      variables: variables,
      context: gqlCommonContext(),
    })

    return res.data
  }

  const onSubmit = async (e: FormEvent<HTMLFormElement>) => {
    e.preventDefault()

    setLoading(true)

    const files: HTMLInputElement[] = Array.from(
      (e.target as HTMLFormElement).querySelectorAll('[type="file"]'),
    )

    const { AttachMutation } = await _presignedURL(files)

    const r = await Promise.all(
      Object.entries(AttachMutation)
        .filter(([k, _]) => k !== '__typename')
        .map(async ([_k, file], idx) => {
          return await _uploadToAzure(file, files[idx].files![0]!)
        }),
    )

    const items = Object.entries(AttachMutation)
      .filter(([k, _]) => k !== '__typename')
      .map(([_, v]) => v.item)

    const res = await _uploadFinish(items)

    setLoading(false)
  }

  return (
    <form onSubmit={onSubmit}>
      {/*<Box mb={4}>*/}
      {/*  <textarea name="content" cols={100} rows={6} />*/}
      {/*</Box>*/}

      <input type="file" name="f1" />
      <input type="file" name="f2" />
      <input type="file" name="f3" />
      <input type="file" name="f4" />
      <input type="file" name="f5" />

      <input type="submit" />
    </form>
  )
}

export { DynamicCreate }
