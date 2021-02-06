package ru.ifmo.rain.mozhevitin.student;

import info.kgeorgiy.java.advanced.student.Group;
import info.kgeorgiy.java.advanced.student.Student;
import info.kgeorgiy.java.advanced.student.StudentGroupQuery;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

public class StudentDB implements StudentGroupQuery {
    private final Comparator<Student> byNameComparator = Comparator.comparing(Student::getLastName)
            .thenComparing(Student::getFirstName)
            .thenComparing(Student::getId);

    @Override
    public List<Group> getGroupsByName(Collection<Student> students) {
        return getGroupsBy(students, sts -> sts.stream().sorted(byNameComparator).collect(Collectors.toList()));
    }

    @Override
    public List<Group> getGroupsById(Collection<Student> students) {
        return getGroupsBy(sortStudentsById(students), Function.identity());
    }

    @Override
    public String getLargestGroup(Collection<Student> students) {
        return getLargestGroupBy(students, List::size);
    }

    @Override
    public String getLargestGroupFirstName(Collection<Student> students) {
        return getLargestGroupBy(students, sts -> getDistinctFirstNames(sts).size());
    }

    @Override
    public List<String> getFirstNames(List<Student> students) {
        return mapStudents(students, Student::getFirstName);
    }

    @Override
    public List<String> getLastNames(List<Student> students) {
        return mapStudents(students, Student::getLastName);
    }

    @Override
    public List<String> getGroups(List<Student> students) {
        return mapStudents(students, Student::getGroup);
    }

    @Override
    public List<String> getFullNames(List<Student> students) {
        return mapStudents(students, s -> s.getFirstName() + " " + s.getLastName());
    }

    @Override
    public List<Student> sortStudentsById(Collection<Student> students) {
        return sortStudentsBy(students, Comparator.comparing(Student::getId));
    }

    @Override
    public List<Student> sortStudentsByName(Collection<Student> students) {
        return sortStudentsBy(students, byNameComparator);
    }

    @Override
    public List<Student> findStudentsByFirstName(Collection<Student> students, String name) {
        return findStudentsBy(students, name, Student::getFirstName);
    }

    @Override
    public List<Student> findStudentsByLastName(Collection<Student> students, String name) {
        return findStudentsBy(students, name, Student::getLastName);
    }

    @Override
    public List<Student> findStudentsByGroup(Collection<Student> students, String group) {
        return findStudentsBy(students, group, Student::getGroup);
    }

    @Override
    public Set<String> getDistinctFirstNames(List<Student> students) {
        return students.stream()
                .map(Student::getFirstName)
                .collect(Collectors.toCollection(TreeSet::new));
    }

    @Override
    public String getMinStudentFirstName(List<Student> students) {
        return students.stream()
                .min(Student::compareTo)
                .map(Student::getFirstName)
                .orElse("");
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(Collection<Student> students, String group) {
        return findStudentsByGroup(students, group)
                .stream()
                .collect(Collectors.toMap(
                        Student::getLastName,
                        Student::getFirstName,
                        (fname1, fname2) -> CharSequence.compare(fname1, fname2) <= 0 ? fname1 : fname2
                ));
    }

    private List<Group> getGroupsBy(Collection<Student> students,
                                    Function<List<Student>, List<Student>> studentsListMapper) {
        return students.stream()
                .collect(Collectors.groupingBy(Student::getGroup))
                .entrySet()
                .stream()
                .map(entry -> new Group(entry.getKey(),
                        studentsListMapper.apply(entry.getValue())))
                .sorted(Comparator.comparing(Group::getName))
                .collect(Collectors.toList());
    }

    private String getLargestGroupBy(Collection<Student> students,
                                     Function<List<Student>, Integer> studentsListMapper) {
        return students.stream()
                .collect(Collectors.groupingBy(Student::getGroup))
                .entrySet()
                .stream()
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        entry -> studentsListMapper.apply(entry.getValue())
                ))
                .entrySet()
                .stream()
                .max(Map.Entry.<String, Integer>comparingByValue()
                        .thenComparing(Map.Entry.comparingByKey(
                                Collections.reverseOrder(String::compareTo)))
                )
                .map(Map.Entry::getKey)
                .orElse("");
    }

    private List<String> mapStudents(List<Student> students, Function<Student, String> mapper) {
        return students.stream()
                .map(mapper)
                .collect(Collectors.toList());
    }

    private List<Student> findStudentsBy(Collection<Student> students,
                                         String key, Function<Student, String> getter) {
        return students.stream()
                .filter(s -> getter.apply(s).equals(key))
                .sorted(byNameComparator)
                .collect(Collectors.toList());
    }

    private List<Student> sortStudentsBy(Collection<Student> students, Comparator<Student> comparator) {
        return students.stream()
                .sorted(comparator)
                .collect(Collectors.toList());
    }
}
