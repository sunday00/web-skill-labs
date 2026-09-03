class Product < ApplicationRecord
  include Notifications
  has_many :subscribers, dependent: :destroy
  has_rich_text :description
  has_one_attached :featured_image

  validates :name, presence: true
  validates :inventory_count, numericality: { greater_than_or_equal_to: 0 }
end
