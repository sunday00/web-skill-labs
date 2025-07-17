import gleam/dict
import gleam/list
import gleam/option
import gleam/result
import gleam/string

pub type School {
  School(students: dict.Dict(Int, List(String)))
}

pub fn create() -> School {
  School(dict.new())
}

pub fn roster(school: School) -> List(String) {
  school.students
  |> dict.to_list
  |> list.flat_map(fn(stu) {
    let #(_grade, name) = stu
    name
  })
}

pub fn add(
  to school: School,
  student student: String,
  grade grade: Int,
) -> Result(School, Nil) {
  case school |> roster() |> list.contains(student) {
    True -> Error(Nil)
    False -> {
      let new_students =
        school.students
        |> dict.upsert(grade, fn(prev_students) {
          [student, ..prev_students |> option.unwrap([])]
          |> list.sort(string.compare)
        })

      Ok(School(new_students))
    }
  }
}

pub fn grade(school: School, desired_grade: Int) -> List(String) {
  dict.get(school.students, desired_grade) |> result.unwrap(list.new())
}
// pub fn main() {
//   let a =
//     create()
//     |> add("Bradley", 5)
//   // |> result.unwrap(School(students: dict.from_list([#(5, [""])])))
//   // |> add("Franklin", 5)
//   // |> result.unwrap(School(students: dict.from_list([#(5, [""])])))
//   // |> grade(5)

//   echo a
// }
