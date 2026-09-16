import { type SandboxedJob } from 'bullmq'

export default async (job: SandboxedJob) => {
  console.log(job.name + ': from external')
}
