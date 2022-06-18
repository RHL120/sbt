("breaks", "echo -n does this test work") should {
	 contain("byeeeee")
	 contain("nope")
	 not("contain(\"fuck\")")
}
("works", "echo -n hello world") should {have_prefix("hello")}
