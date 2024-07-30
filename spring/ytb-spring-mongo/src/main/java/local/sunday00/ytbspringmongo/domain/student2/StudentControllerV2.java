package local.sunday00.ytbspringmongo.domain.student2;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;

import java.util.Optional;

@RestController()
@RequestMapping(path = "/v2")
public class StudentControllerV2 {
    private final Student2Repository studentRepository;
    private final MongoTemplate mongoTemplate;

    public StudentControllerV2(
            Student2Repository studentRepository,
            MongoTemplate mongoTemplate
    ) {
        this.studentRepository = studentRepository;
        this.mongoTemplate = mongoTemplate;
    }

    @PostMapping("/student")
    public Student createStudent(@RequestBody RequestStudentDto dto) {
        Student student = new Student(
            dto.getFirstName(),
            dto.getLastName(),
            dto.getEmail(),
            dto.getGender()
        );

        student.setAddress(dto.getAddress());
        student.setFavoriteSubjects(dto.getFavoriteSubjects());
        student.setTotalSpentInBooks(dto.getTotalSpentInBooks());

        return studentRepository.save(student);
    }

    @GetMapping("/student/{id}")
    public Student findStudentWithQuery(@PathVariable("id") String id) {
        Query query = new Query();
        query.addCriteria(Criteria.where("id").is(id));

        return this.mongoTemplate.findOne(query, Student.class);
    }

    @GetMapping("/student/email/{email}")
    public Optional<Student> findStudentByEmail(@PathVariable("email") String email) {
        Optional<Student> result = this.studentRepository.findStudentsByEmail(email);

        result.ifPresentOrElse(System.out::println, () -> {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "notExists");
        });

        return result;
    }
}
