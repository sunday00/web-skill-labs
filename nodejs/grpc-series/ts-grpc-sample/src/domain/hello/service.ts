import { Message } from '@bufbuild/protobuf'

export class HelloService {
  sendMessage(message: string): Message {
    const conversation = this.conversations.get(conversationId)
    if (!conversation) {
      throw new Error('Conversation not found')
    }

    const userMessage: Message = {
      id: randomUUID(),
      createdAt: new Date(),
      role: 'user',
      content: content,
    }
    conversation.messages.push(userMessage)

    const botMessage: Message = {
      id: randomUUID(),
      createdAt: new Date(),
      role: 'bot',
      content: 'Hello from Connect!',
    }
    conversation.messages.push(botMessage)

    return botMessage
  }
}
