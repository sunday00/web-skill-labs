import { Injectable } from '@nestjs/common';
import { PrismaService } from '../common/prisma/prisma.service';
import { Article, Prisma } from '@prisma/client';

@Injectable()
export class ArticleService {
  constructor(private readonly prismaService: PrismaService) {}

  async getArticle(id: number) {
    return this.prismaService.article.findFirst({
      where: { id },
    });
  }

  async createArticle(body: Partial<Article>) {
    return this.prismaService.article.create({
      data: {
        slug: body.slug,
        title: body.title,
        content: body.content,
        createdAt: new Date(),
        updatedAt: new Date(),
      },
    });
  }

  async getArticleUnsafe(id: number) {
    console.log(`
      SELECT id, title, content, "createdAt" FROM core."Article" WHERE id=${id}
    `);

    return this.prismaService.$queryRawUnsafe(
      `
      SELECT id, title, content, "createdAt" FROM core."Article" WHERE id=$1
    `,
      id,
    );
  }

  async getArticleSafe(id: number) {
    console.log(`
      SELECT id, title, content, "createdAt" FROM core."Article" WHERE id=${id}
    `);

    return this.prismaService.$queryRaw(
      Prisma.sql`SELECT id, title, content, "createdAt" FROM core."Article" WHERE id=${id}`,
    );
  }
}
