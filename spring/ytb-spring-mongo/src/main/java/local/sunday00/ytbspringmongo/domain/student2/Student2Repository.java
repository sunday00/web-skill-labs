package local.sunday00.ytbspringmongo.domain.student2;

import org.springframework.data.mongodb.repository.MongoRepository;

import java.util.Optional;

public interface Student2Repository extends MongoRepository<Student, String> {
    Optional<Student> findStudentsByEmail(String email);
}
