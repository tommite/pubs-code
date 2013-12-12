package fi.smaa.prefsel;

public interface AnswerNode extends Node<AnswerNode, QuestionNode>{

	public static final int NO_ANSWER = -1;
	public abstract QuestionNode[] getChildren();
	public int getAnswer();

}