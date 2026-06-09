import { gql } from '@apollo/client'
import type { FormEvent } from 'react'
import { useMutation } from '@apollo/client/react'
import { gqlCommonContext } from '@/utils/gql.util.ts'
import { Box, Spinner } from '@chakra-ui/react'
import { useNavigate } from 'react-router'
import type { PresignedUrl } from '@/graphql/types.ts'

const q = gql`
  mutation AttachPresign(
    $input1: AttachItem!
    $input2: AttachItem!
    $input3: AttachItem!
    $input4: AttachItem!
    $input5: AttachItem!
  ) {
    AttachMutation {
      f1: attachPresign(input: $input1) {
        item {
          sortKey
          id
          upload
          sig
          cdn
        }
        success
      }
      f2: attachPresign(input: $input2) {
        item {
          sortKey
          id
          upload
          sig
          cdn
        }
        success
      }
      f3: attachPresign(input: $input3) {
        item {
          sortKey
          id
          upload
          sig
          cdn
        }
        success
      }
      f4: attachPresign(input: $input4) {
        item {
          sortKey
          id
          upload
          sig
          cdn
        }
        success
      }
      f5: attachPresign(input: $input5) {
        item {
          sortKey
          id
          upload
          sig
          cdn
        }
        success
      }
    }
  }
`

const q2 = gql`
  mutation AttachUploaded(
    $input1: AttachUploadedInput!
    $input2: AttachUploadedInput!
    $input3: AttachUploadedInput!
    $input4: AttachUploadedInput!
    $input5: AttachUploadedInput!
  ) {
    AttachMutation {
      f1: attachUploaded(input: $input1) {
        item {
          id
        }
        success
      }
      f2: attachUploaded(input: $input2) {
        item {
          id
        }
        success
      }
      f3: attachUploaded(input: $input3) {
        item {
          id
        }
        success
      }
      f4: attachUploaded(input: $input4) {
        item {
          id
        }
        success
      }
      f5: attachUploaded(input: $input5) {
        item {
          id
        }
        success
      }
    }
  }
`

const q3 = gql`
  mutation Mutation($input: CreateFeedInput!) {
    FeedMutation {
      createFeed(input: $input) {
        item {
          id
        }
        success
      }
    }
  }
`
const AttachCreate = () => {
  const navigate = useNavigate()

  const [presignMt, { loading, error }] = useMutation<{
    AttachMutation: { [k: string]: { item: PresignedUrl } }
  }>(q, {
    context: gqlCommonContext(),
    // onCompleted: (data) => {
    //   // navigate('/proam/attach/id')
    //   console.log(data)
    // },
  })

  const [uploadedMt, { loading: l2, error: e2 }] = useMutation(q2, {
    context: gqlCommonContext(),
    // onCompleted: (data) => {
    //   // navigate(`/proam/attach/id`)
    //   console.log(data)
    // },
  })

  const [createMt, { loading: l3, error: e3 }] = useMutation(q3, {
    context: gqlCommonContext(),
    onCompleted: (data) => {
      return navigate(
        `/proam/attach/${
          (
            data as {
              FeedMutation: { createFeed: { item: { id: string } } }
            }
          ).FeedMutation.createFeed.item.id
        }`,
      )
    },
  })

  if (loading || l2 || l3) return <Spinner />

  if (error) {
    alert(error.message)
  }

  if (e2) {
    alert(e2.message)
  }

  if (e3) {
    alert(e3.message)
  }

  const onSubmit = async (e: FormEvent<HTMLFormElement>) => {
    e.preventDefault()
    const input = e.currentTarget.elements

    const presigneds = await presignMt({
      variables: {
        input1: {
          originalExt: 'png',
          originalName: 'no1',
          sortKey: 1,
          type: 'IMAGE',
        },
        input2: {
          originalExt: 'png',
          originalName: 'no2',
          sortKey: 2,
          type: 'IMAGE',
        },
        input3: {
          originalExt: 'png',
          originalName: 'no3',
          sortKey: 3,
          type: 'IMAGE',
        },
        input4: {
          originalExt: 'png',
          originalName: 'no4',
          sortKey: 4,
          type: 'IMAGE',
        },
        input5: {
          originalExt: 'png',
          originalName: 'no5',
          sortKey: 5,
          type: 'IMAGE',
        },
      },
    })

    const uploadedMtInputs: { id: string; sortKey: number }[] = []
    const presignedObjectsArr = Object.entries(presigneds.data!.AttachMutation)
      .filter((a) => a[0] !== '__typename')
      .map((a) => a[1])

    await Promise.all(
      Array.from(
        (e.target as HTMLFormElement).querySelectorAll('[type="file"]'),
      ).map(async (formF, idx) => {
        const presigned = presignedObjectsArr.find(
          (v) => v.item.sortKey === idx + 1,
        )

        const url = `${presigned!.item.upload}/${presigned!.item.id}.png?${presigned!.item.sig}`
        await fetch(url, {
          method: 'PUT',
          headers: {
            ContentType: 'image/png',
            'x-ms-blob-type': 'BlockBlob',
          },
          body: (formF as HTMLInputElement)!.files![0]!,
        })

        uploadedMtInputs.push({
          id: presigned!.item.id,
          sortKey: presigned!.item.sortKey,
        })
      }),

      // Object.entries(presigneds.data!.AttachMutation)
      //   .filter((a) => a[0] !== '__typename')
      //   .sort((a, b) => b[1].item.sortKey - a[1].item.sortKey)
      //   .map(async ([k, v]) => {
      //     if (!v.item) return
      //
      //     const url = `${v.item.upload}/${v.item.id}.png?${v.item.sig}`
      //
      //     const file = (input.namedItem(k) as HTMLInputElement).files?.[0]
      //     if (!file) return
      //
      //     await fetch(url, {
      //       method: 'PUT',
      //       headers: {
      //         ContentType: 'image/png',
      //         'x-ms-blob-type': 'BlockBlob',
      //       },
      //       body: file,
      //     })
      //
      //     uploadedMtInputs.push({ id: v.item.id, sortKey: v.item.sortKey })
      //   }),
    )

    const inputs: any = {}

    uploadedMtInputs.forEach(async (i) => {
      inputs[`input${i.sortKey}`] = {
        id: i.id,
        sizes: [{ name: 'full', wh: [1, 2] }],
      }
    })

    await uploadedMt({
      variables: inputs,
    })

    return await createMt({
      variables: {
        input: {
          clubId: '6a0fb2edf062b28e7e1a15e2',
          content: (input.namedItem('content') as HTMLTextAreaElement).value,
          type: 'FAN',
          attachInputs: uploadedMtInputs,
        },
      },
    })
  }

  return (
    <form onSubmit={onSubmit}>
      <Box mb={4}>
        <textarea name="content" cols={100} rows={6} />
      </Box>

      <input type="file" name="f1" />
      <input type="file" name="f2" />
      <input type="file" name="f3" />
      <input type="file" name="f4" />
      <input type="file" name="f5" />

      <input type="submit" />
    </form>
  )
}

export { AttachCreate }
