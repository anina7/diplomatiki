import hypothesis.strategies as st
from hypothesis import given

@st.composite
def gen(draw):
	x = draw(st.integers())
	return x
	
@given(s1=gen(), s2=gen()) # <===== change
def test_subtraction(s1, s2):

    print(s1, s2)

    #assert 0
	
test_subtraction()