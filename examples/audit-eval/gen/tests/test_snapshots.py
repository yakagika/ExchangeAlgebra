from __future__ import annotations

import hashlib
import sys
from pathlib import Path

import pytest

EVAL_DIR = Path(__file__).resolve().parents[2]
if str(EVAL_DIR) not in sys.path:
    sys.path.insert(0, str(EVAL_DIR))

from gen.defects import generate_audit_task
from gen.generate import dump_json, generate_task


SNAPSHOTS = {
    "journalize": {
        "cash_sale": "a6b7fa751ec8bdd4fbad63c21b216d27ffcd4d77efad25bedc947da4dfb87bcc",
        "credit_trade": "bee1ed7ba332838a7a5d0dc177690b082d82b910b448a8b5e3ae8887a6a86183",
        "purchase": "a1f8a1e692961c49fe143405642bec5c2f93f95518cabcd14d94d37f546f6366",
        "payroll": "945e6033113f526acfdfc83284d8b38f8cc0eed66b84eeea8cb0dc43396e9b9b",
        "accrual": "8e4df7db92efb77052ce92a57c2ee87d6ca6864017d1d204873035e28c4bf383",
        "tax": "5a269a00f4ede31243edee954d5f05d52321b98bf4e7fa405195fb17f0c8a0fc",
        "fixed_asset": "e44aa5eae52c8e0648b0242a360143029522f1f65391889e75374e8fbee98ce9",
        "mixed": "e51e54a93da4286ee6f5e5545a3f890b3056c3333a283d09a057bb734cd80bf4",
    },
    "audit": {
        "cash_sale": "2983ae933f6ed1ee23a99e893a63c7b41e94018cd595288758bba806c107c582",
        "credit_trade": "c3e4d785701d431fe83a45c67fd2f71632b50a779be43771fcf48c33ce72a580",
        "purchase": "2b15e39f5330b2e9972448509e23bd75d0b8635af05aedc8c5fc4094dfd06c90",
        "payroll": "0fbbede7c8dca8d85d215a61a7cf63f7bdfaf17c6503c173e4ff9a33fe158b59",
        "accrual": "cfdbbc34ecac435171cb24ca54cde3ba62881506490ad098beda8664085e3451",
        "tax": "5d39afa5fc856a2ca03f99f85bef42ca9642bd106a83c9a77d7845ff70aa8c8e",
        "fixed_asset": "c6d7fb2b115aca8a126fff7ce12d11bf6d240c6827085ab0464898ef852ee000",
        "mixed": "3e2c7160ecdacc9674ae1b26f1820ef9c9ab998bd55d72146662fbe8ca38fadf",
    },
}


@pytest.mark.parametrize(
    ("kind", "template", "expected"),
    [
        (kind, template, expected)
        for kind, templates in SNAPSHOTS.items()
        for template, expected in templates.items()
    ],
)
def test_existing_generated_task_bytes_are_frozen(
    kind: str,
    template: str,
    expected: str,
) -> None:
    """Protect experiment-1 task bytes from generator refactors."""
    if kind == "journalize":
        task = generate_task(seed=0, count=5, template=template)
    else:
        task = generate_audit_task(seed=0, count=5, defects=2, template=template)

    actual = hashlib.sha256(dump_json(task).encode("utf-8")).hexdigest()
    assert actual == expected
