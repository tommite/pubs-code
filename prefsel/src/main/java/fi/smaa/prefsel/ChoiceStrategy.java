package fi.smaa.prefsel;

public interface ChoiceStrategy {
	
	public QuestionNode getNextQuestion(Question[] questions);
}
