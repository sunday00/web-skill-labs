package local.sunday00.ytbspringmongo.domain.student;

import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class StudentController {
    private final StudentRepository studentRepository;

    public StudentController(StudentRepository studentRepository) {
        this.studentRepository = studentRepository;
    }

    @PostMapping("student")
    public void addStudent(@RequestBody RequestDto params) {
        Student student = new Student();
        student.setRno(params.getRno());
        student.setName(params.getName());
        student.setAddress(params.getAddress());

        this.studentRepository.save(student);
    }
}
